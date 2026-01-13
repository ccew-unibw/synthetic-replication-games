FROM python:3.13-slim

RUN apt-get update && apt-get install -y git

COPY --from=docker.io/astral/uv:latest /uv /uvx /bin/

WORKDIR /app
COPY . .

RUN uv sync --frozen

ENTRYPOINT ["uv", "run", "talkingtomachines"]
