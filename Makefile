.PHONY: run

run:
	export $$(cat .env | xargs) && dotnet run --project app $$TOKEN $$OWNER $$ORIGIN
