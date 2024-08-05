{
  project.name = "weather-caching-proxy";
  services.redis = {
    service.image = "redis:7.4";
    service.ports = ["6379:6379"];
    service.environment.REDIS_PASSWORD = "redis";
  };
}
