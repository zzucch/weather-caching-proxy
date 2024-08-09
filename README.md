# weather-caching-proxy

This is a server application that implements proxying with caching for a weather service. 
The server provides an HTTP API with a single method for retrieving the weather at a specified location and time.
The Open-Meteo API (https://api.open-meteo.com/v1/) is used for accessing weather data.

When a request is made to the server, it uses cache if the data is present (with possible location and time approximations). 
Weather data from the past can only be loaded from cache.
The cache can be automatically populated for a given list of locations with a specified interval.
The server supports OpenAPI specification (servant-swagger).

## Extarnal API

The API key is extracted from an environment variable (for free Open-Meteo API usage `API_KEY` should be equal to `""` - see `.env.example` file for details).

The domain for the external API can be specified in configuration file.

## Configuration

The server accepts the following parameters (from a `config.dhall` configuration file):

- Port number on which the server will run
- Proximity offsets for coordinates and time
- List of locations for auto-caching
- Period for auto-cache updates
- Offset for what to consider "current" time
- (Optional) Custom external API domain

## Usage

Run following command to start the server:

```bash
stack run
```

For running cache storage (Redis) run the following commands:

```bash
cd deployments

sudo arion up
```

To get curent weather in Moscow, Russia run the following command:

```bash
curl "http://localhost:8081/weather?latitude=55.751244&longitude=37.618423&time=${EPOCHREALTIME::-7}"
```
