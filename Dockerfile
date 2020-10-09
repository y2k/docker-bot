FROM mcr.microsoft.com/dotnet/sdk:5.0.100-rc.1-alpine3.12

WORKDIR /app
COPY . /app

RUN cd test && dotnet test
RUN cd app && dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:5.0.0-rc.1-alpine3.12

WORKDIR /app
COPY --from=0 /app/app/bin/Release/net5.0/linux-x64/publish .

ENTRYPOINT ["dotnet", "app.dll"]
