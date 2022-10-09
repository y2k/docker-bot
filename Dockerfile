FROM mcr.microsoft.com/dotnet/sdk:6.0.401-alpine3.16

WORKDIR /app
COPY . /app

RUN cd test && dotnet test
RUN cd app && dotnet publish -c Release -r linux-x64 --self-contained false

FROM mcr.microsoft.com/dotnet/runtime:6.0.9-alpine3.16

WORKDIR /app
COPY --from=0 /app/app/bin/Release/net6.0/linux-x64/publish .

ENTRYPOINT ["dotnet", "app.dll"]
