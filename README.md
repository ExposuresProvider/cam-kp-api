[![Scala CI](https://github.com/ExposuresProvider/cam-kp-api/actions/workflows/scala.yml/badge.svg)](https://github.com/ExposuresProvider/cam-kp-api/actions/workflows/scala.yml)

# cam-kp-api

## Introduction

CAMs (Causal Activity Models) are small knowledge graphs built using the [Web Ontology Language (OWL)](https://www.w3.org/OWL/). The CAM database combines many CAM graphs along with a large merged bio-ontology containing the full vocabulary of concepts referenced within the individual CAMs. Each CAM describes an instantiation of some of those concepts in a particular context, modeling the interactions between those instances as an interlinked representation of a complex biological or environmental process.

[Click here for the term definitions.](https://docs.google.com/spreadsheets/d/1C8hKXacxtQC5UzXI4opQs1r4pBJ_5hqgXrZH_raYQ4w/edit#gid=1581951609)

## Query Process

Anyone can query the database by posting the query here: [CAM-KP API](https://stars-app.renci.org/cam-kp/docs/index.html?url=docs.yaml#/default/postQuery). Click here for some [example queries](https://github.com/NCATS-Tangerine/cam-kp-api/wiki/Example-Queries).

[SmartAPI Registration](https://smart-api.info/registry?q=230691056df158545fd38bb73379d9c3)

## Running

CAMs (Causal Activity Models) are small knowledge graphs built using the [Web Ontology Language (OWL)](https://www.w3.org/OWL/). The CAM database combines many CAM graphs along with a large merged bio-ontology containing the full vocabulary of concepts referenced within the individual CAMs. Each CAM describes an instantiation of some of those concepts in a particular context, modeling the interactions between those instances as an interlinked representation of a complex biological or environmental process. 

- `sbt docker:stage docker:publishLocal`
- `docker run -p 8080:8080 -it renciorg/cam-kp-api:0.1` (or detached mode: `docker run -p 8080:8080 -d renciorg/cam-kp-api:0.1`)

## Issue Management

Anyone can create new issues by clicking on "New issue" button on the [issues](https://github.com/NCATS-Tangerine/cam-kp-api/issues) page of this repository. Click here for the [current issues](https://github.com/NCATS-Tangerine/cam-kp-api/issues).

## Example queries

Example queries are included in [./src/it/resources/examples](./src/it/resources/examples).
Look for the `message` field in each JSON file in that directory. You can run them by running:

```shell
$ CAM_KP_ENDPOINT=https://cam-kp-api-dev.renci.org/1.2.0/query sbt "IntegrationTest/testOnly org.renci.cam.it.ExampleQueriesEndpointTest"
```

Server responses will be written to the `./src/it/resources/example-results` directory.
