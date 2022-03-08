[![Build Status](https://travis-ci.com/NCATS-Tangerine/cam-kp-api.svg?branch=master)](https://travis-ci.com/NCATS-Tangerine/cam-kp-api)

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

## Testing

Two types of tests are included in this repository:
- Unit tests can be run by running `sbt test`
- Integration tests require Docker. To run them:
  1. Create the Docker image for cam-kp-api by running `sbt docker:stage docker:publishLocal`
  2. Run the integration tests by running `sbt IntegrationTest/test`.
     You can also run a single test by running e.g. `sbt "IntegrationTest/testOnly org.renci.cam.it.BiolinkTest"`

## Issue Management

Anyone can create new issues by clicking on "New issue" button on the [issues](https://github.com/NCATS-Tangerine/cam-kp-api/issues) page of this repository. Click here for the [current issues](https://github.com/NCATS-Tangerine/cam-kp-api/issues).
