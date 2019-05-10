-- seed SQL file used for testing purposes

-- inserts manager with the name 'admin' and hash of the password '123'
INSERT INTO users (email, name, pwd_hash)
VALUES ('test@test.com', 'User Userov', '$2y$14$dkHVYLbmnTTeT8DUuAsM5uOz7djmrcuzUv5jXurxveUk0vRca/AqW');
