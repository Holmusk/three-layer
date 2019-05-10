ide:
	ghcid --command "stack ghci --ghci-options=-fno-code --main-is three-layer:generate-elm three-layer:lib three-layer:exe:three-layer-exe three-layer:test:three-layer-test"

postgres:
	docker run -p 5432\:5432 -e POSTGRES_USER=root -e POSTGRES_DB=three-layer postgres\:10.5-alpine

sql-repl:
	psql -h localhost -p 5432 -U root -d three-layer
