# Getting Started w/ Docker & Kubernetes at RENCI 

### My local env:

```
> lsb_release -d && cat /proc/meminfo | grep MemTotal && cat /proc/cpuinfo | grep processor | wc -l
Description:    Ubuntu 20.10
MemTotal:       48964784 kB
8
```

## Prequisite software
* Install Docker (https://docs.docker.com/engine/install/ubuntu/) 
* Create a Docker Hub account (https://hub.docker.com/)
* Install Kubernetes kubectl (https://kubernetes.io/docs/tasks/tools/install-kubectl/) 
* Install Keybase (https://keybase.io/)
* Install the RENCI VPN (https://wiki.renci.org/index.php/VPN)
  
## Steps
1. Login to Docker locally...run: ```docker login```
2. Email Phil to have your Docker account associated with the ```renciorg``` organization with write permissions.
3. Submit a ticket in help.renci.org requesting an account (kube config) and personal namespace on Mitchel (prod) and BlackBalsam (dev).  Also request to be added to the translator & translator-dev namespaces & include your keybase username in the ticket such that you can be securely sent kube config files.
4. PJ Linebaugh (pjl_renci in Keybase) will likely pick up the ticket & send you kube config files via Keybase.  Put those config files in a local directory ```~/.kube```.
5. Start your VPN.
6. Specify the context you wish to use...run: ```kubectl config use-context <kubeConfigFile>```.  Also, you can login to https://dashboard.blackbalsam-cluster.edc.renci.org/#/login using one of your kube config files to explore. 
   If all is working properly, running ```kubectl config view``` should yield the following:
```
apiVersion: v1
clusters:
- cluster:
    certificate-authority-data: DATA+OMITTED
    server: <server>
  name: blackbalsam-cluster
contexts:
- context:
    cluster: blackbalsam-cluster
    namespace: <personal namespace>
    user: <username>
  name: <username>
current-context: <username>
kind: Config
preferences: {}
users:
- name: <username>
  user:
    client-key-data: REDACTED
    token: REDACTED
```
7. Apply deployment, service, and ingress yaml files: 
   1. ```kubectl apply -f deployment.yaml```
   2. ```kubectl apply -f service.yaml```
   3. ```kubectl apply -f ingress-dev.yaml``` or ```kubectl apply -f ingress.yaml```
8. From your browser, request https://cam-kp-api.renci.org/predicates or https://cam-kp-api-dev.renci.org/predicates

### Notes
* One way to assert that the container is running is to:
  1. Login to the dashboard (see step 6 above)
  2. Select your namespace from the left menu dropdown
  3. Click on "Workloads" -> "Pods" from the left navigation
  4. Click on the ellipsis on the right for the running cam-kp-api instance and select "Exec"
  5. From the bash prompt, run ```curl -X GET http://localhost:8080/predicates``` which should return the predicates for this service.
  6. If the container is running & the host is not reachable, then contact PJ to debug.
* Publishing to the renciorg namespace in docker should align with the deployment file's image value

### Deploy CAM-KP-API to Docker
* Run the following to create a docker image locally
  * ```sbt docker:stage```
* Run the following to deploy an image to your local Docker repository
  * ```sbt docker:publishLocal```
* Run the following to deploy an image to hub.docker.com
  * ```sbt docker:publish```
