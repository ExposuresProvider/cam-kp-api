[![Build Status](https://travis-ci.com/NCATS-Tangerine/cam-kp-api.svg?branch=master)](https://travis-ci.com/NCATS-Tangerine/cam-kp-api)

# cam-kp-api

## Running

### Docker

- `sbt docker:stage`
- `sbt docker:publishLocal`
- `docker run -p 8080:8080 -it cam-kp-api:0.1` (or detached mode: `docker run -p 8080:8080 -d cam-kp-api:0.1`)
