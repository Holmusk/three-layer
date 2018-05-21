[![CircleCI](https://circleci.com/gh/Holmusk/three-layer.svg?style=svg)](https://circleci.com/gh/Holmusk/three-layer)
# three-layer-servant-starter

This package is aimed at being a batteries included starting template for writing web servers using `servant`. It follows the approach detailed [here](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html).

Things that are included:
* Custom prelude (uses [`universum`](https://github.com/serokell/universum))
* Logging integration (uses `katip`)
* Database helper functions (uses `postgresql-simple`)
* Monitoring time taken for your `App` actions (uses `ekg`)
* Password hashing functions (uses `bcrypt`)
* JWT helper functions (uses `jwt`)
* Testing support (uses `tasty` and `hedgehog`)

[TODO]: rewrite code below according to libraries we use

## Getting started

### AppEnv
Append to the `AppEnv` type found at `Lib/App/Env.hs` with any other values you want it to hold.

Modify the `mkAppEnv` function at `Lib.hs` to match any changes you have made

### AppError
Add any other type constructors you want to `AppError` type found at `Lib/App/Error.hs`.

Modify the `runAppAsHandler` function at `App.hs` to map your errors into http error codes.

## Adding an end point

Here are the steps for adding a hypothetical endpoint that allows for searching for animals given an UUID.

We will first create an effect that represents the ability to get an animal from the database.

Create a new file `Lib/Effects/Animal.hs`:

```haskell
module Lib.Effects.Animal where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.UUID.Types                    (UUID)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Lib.App.Env
import           Lib.App.Error
import           Lib.Util.App

class (MonadReader AppEnv m, MonadError AppError m, MonadIO m) => MonadAnimal m where
  getAnimalById :: UUID -> m (Maybe Animal)
  getAnimalById animalId = timedAction "getAnimal" $ head <$> queryPG [sql|
      SELECT
        *
      FROM
        animals
      WHERE
        id = ?
    |] [animalId]

data Animal = Animal {
  animalId    :: UUID,
  animalName  :: Text
} deriving (Generic)

instance ToJSON Animal
instance FromJSON Animal

instance FromRow Animal where
  fromRow = do
    animalId <- field
    animalName <- field
    return Animal{..}
```

We define a default implementation of `getAnimal` which fetches the animal from the database. We can use a different instance to alter this behaviour when the handler we write for the server is run in tests.

We should also add a line to `App.hs` to give our `App` type an instance for our new `MonadAnimal` effect:

```haskell
instance MonadAnimal App
```

Create a new file `Lib/Server/Animal.hs`:

```haskell
module Lib.Server.Animal where

import           Control.Monad.Logger
import           Data.Aeson           (FromJSON, ToJSON)
import           Lib.App              (App, AppEnv (..), AppError (..),
                                       Session (..))
import           Lib.Effects.Animal
import           Lib.Util.App
import           Lib.Util.JWT
import           Lib.Util.Password
import           Servant.API
import           Servant.Server

newtype AnimalRequest = AnimalRequest {
  animalRequestId :: UUID
} deriving (Generic)

instance FromJSON AnimalRequest
instance ToJSON AnimalRequest

type AnimalAPI = "animal" :> ReqBody '[JSON] AnimalRequest :> Post '[JSON] Animal

animalServer :: ServerT AnimalAPI App
animalServer = getAnimal

getAnimal :: (MonadAnimal m, MonadLogger m) => UUID -> m Animal
getAnimal AnimalRequest{..} = do
  mAnimal <- getAnimalById animalRequestId
  when (isNothing mAnimal) $ do
    $(logDebug) $ "Unable to find animal given animalId " <> animalRequestId
    throwError NotFound
  let (Just animal) = mAnimal
  return animal
```

We can wire up our new route by modifying `Server.hs` with the following lines:

```haskell
import           Lib.Server.Animal

type API = AuthAPI :<|> AnimalAPI

combinedServers = authServer :<|> animalServer
```

Finally lets add some tests for this end point. We want to check that if you ask for an animal which does not exist, the endpoint should throw a 404.

We will create a `AnimalMock` which will use most of the default instances of app except for the `MonadAnimal` instance which we will write as a pure function. This makes testing fast and straightforward.

Create a new file `test/AnimalSpec.hs`:

```haskell
module AnimalSpec where

import           Control.Monad.Logger
import qualified Data.UUID.Types      as UUID
import           Lib.App
import           Lib.Effects.Animal
import           Lib.Server.Animal
import           Test.Tasty
import           Test.Tasty.Hspec
import qualified System.Metrics       as Metrics
import           Data.IORef

newtype AnimalMock a = AnimalMock {
  unAnimalMock :: NoLoggingT (ReaderT AppEnv (ExceptT AppError IO)) a
} deriving (Functor, Applicative, Monad, MonadError AppError, MonadReader AppEnv, MonadIO, MonadLogger)

runAnimalMock :: AnimalMock a -> IO (Either AppError a)
runAnimalMock action = do
  timings <- newIORef HashMap.empty
  ekgStore <- Metrics.newStore
  runExceptT $ runReaderT (runNoLoggingT $ unAnimalMock action) AppEnv{..}

dumbo :: Animal
dumbo = Animal {
  animalId = UUID.nil,
  animalName = "Dumbo"
}

instance MonadAnimal AnimalMock where
  getAnimalById UUID.nil = return $ Just dumbo
  getAnimalById _ = return Nothing

spec_animalSpec :: Spec
spec_animalSpec describe "animal handler" $ do
  it "should return Dumbo given the correct id" $
    runAnimalMock (getAnimal $ AnimalRequest UUID.nil)
      `shouldReturn` Right dumbo
  it "should throw a 404 on an unknown id" $
    runAnimalMock (getAnimal $ AnimalRequest "ff6fbdda-9cba-4745-8408-a90b0debdd94")
      `shouldReturn` Left NotFound
```
