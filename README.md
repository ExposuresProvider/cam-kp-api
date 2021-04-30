[![Build Status](https://travis-ci.com/NCATS-Tangerine/cam-kp-api.svg?branch=master)](https://travis-ci.com/NCATS-Tangerine/cam-kp-api)

# cam-kp-api

## Introduction

CAMs (Causal Activity Models) are small knowledge graphs built using the [Web Ontology Language (OWL)](https://www.w3.org/OWL/). The CAM database combines many CAM graphs along with a large merged bio-ontology containing the full vocabulary of concepts referenced within the individual CAMs. Each CAM describes an instantiation of some of those concepts in a particular context, modeling the interactions between those instances as an interlinked representation of a complex biological or environmental process.

## Querying the database

Anyone can query the database by posting the query here: [CAM-KP API](https://stars-app.renci.org/cam-kp/docs/index.html?url=docs.yaml#/default/postQuery). Click here for some [example queries](https://github.com/NCATS-Tangerine/cam-kp-api/wiki/Example-Queries).

## Running

### Docker

- `sbt docker:stage`
- `sbt docker:publishLocal`
- `docker run -p 8080:8080 -it renciorg/cam-kp-api:0.1` (or detached mode: `docker run -p 8080:8080 -d renciorg/cam-kp-api:0.1`)
