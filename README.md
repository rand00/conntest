# `conntest`

A MirageOS unikernel that acts as a client and server to 
other instances of itself, either via `TCP` or `UDP`. It automatically 
reconnects when the connection is lost, so you can e.g. take down an instance 
and reconfigure it via CLI.

### Usecases
* Testing that your networking setup works - e.g. when you use  bridges, 
  `TAP` devices, `NAT`, firewall rules etc.
* Testing the connection-stats between different servers, MirageOS compilation-targets and network-stacks.
  * This can e.g. be useful if you plan to rearrange where/how your unikernel instances are run.
* Stress-testing connections by sending lots of data to/from several instances
  at the same time.
* Playing around with distributed unikernel setups.

### Work in progress
* A `notty` CLI UI (via `mirage-console`) listing connections and their stats
  * But having the possibility of choosing a simple logging output 
* Show stats:
  * bandwidth 
  * latency
  * lost packets (`UDP`)
  * packets out of order (`UDP`)

### Suggest features!

If you find this unikernel useful, but it's e.g. missing some stats that you are interested in - then make an issue/PR (: 


## Compiling

```
opam install 'mirage>=4.0'
git clone https://github.com/rand00/conntest
cd conntest
mirage configure -t <TARGET> -f mirage/config.ml && make depend && mirage build -f mirage/config.ml
```
Targets:
* For `spt` you need to run GNU/Linux and install `solo5`. 
* For `hvt` you need a baremetal server (i.e. supporting virtualization) or a VPS supporting nested virtualization. Also depends on `solo5`.

## CLI help

After compiling for `unix` - you can get a manual page, where the most 
relevant runtime parameters are under 
`UNIKERNEL PARAMETERS` and `APPLICATION OPTIONS`:
```
mirage/dist/conntest --help
```

## Running 

### `unix` target

Starting a `conntest` that only listens:
```
mirage/dist/conntest --name receiver --listen tcp:1234
```

Starting another `conntest` that connects to the listening instance, 
and does bandwidth monitoring:
```
mirage/dist/conntest --name sender --connect 'tcp://127.0.0.1:1234?monitor-bandwidth&packet-size=5mb'
```

### `spt` target / Linux seccomp 

Starting a `conntest` that only listens:
```
solo5-spt --net:service=tap100 mirage/dist/conntest.spt --ipv4 10.0.0.2/24 --name miav0 --listen tcp:1234
```

#### Networking setup

The following script exemplifies how to setup the bridge and tap-devices to be able to run `spt` unikernels locally. 
Note that further `iptables` setup to NAT the traffic from your single public ip is needed to expose the unikernels 
on the internet:

``` bash
#! /bin/bash

set -e

sudo -i

#> // Bridge setup

ip link add service type bridge
ip addr add 10.0.0.1/24 dev service
ip link set dev service up

#> // Tap devices setup per ip (instead of albatross doing it)

ip tuntap add tap100 mode tap
ip addr add 10.0.0.2/24 dev tap100
ip link set dev tap100 up
ip link set tap100 master service

ip tuntap add tap101 mode tap
ip addr add 10.0.0.3/24 dev tap101
ip link set dev tap101 up
ip link set tap101 master service

#> // Check if works
ip link
bridge link
```
