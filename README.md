# `conntest`

A MirageOS unikernel that acts as a client and server to 
other instances of itself, either via `TCP` or `UDP`. You specify 
which ports to listen/connect to, and which stats to collect. It automatically 
reconnects when the connection is lost, so you can e.g. take down an instance 
and reconfigure it via CLI. 

An important aspect is that each `conntest` unikernel is connected via a single
connection only - which is used in turn for the different tests. This allows you
to simulate how your production-unikernels will be connected.

### Usecases
* Testing that your networking setup works - e.g. when you use  bridges, 
  `TAP` devices, `NAT`, firewall rules etc. which can become complex. 
* As an alternative to `nmap` for testing what ports are open in a firewall - seen 
  from a unikernels vantagepoint. 
  * Note that `nmap` can't be attached directly to a `TAP` device, 
    so you need to wrap it in a VM to be run from the same positions as is possible for 
    a unikernel.
* Testing the connection-stats between different servers, MirageOS compilation-targets 
  and network-stacks.
  * This can e.g. be useful:
    * if you plan to rearrange where/how your unikernel instances are run
    * if you compare performance of different MirageOS backends
  * Note that e.g. `iperf`, like `nmap`, also need to be wrapped in a VM to test 
    performance from the same positions as a unikernel.
* Stress-testing connections by sending lots of data to/from several instances
  at the same time.
* Playing around with distributed unikernel setups.

### Features
* Connect with as many `conntest` servers/clients as you want.
* The `conntest` protocol is abstracted on top of `TCP`/`UDP`:
  * the `TCP` protocol is used as-is, where `conntest` adds a custom packet format on top
  * the `UDP` protocol is extended with `TCP`-like semantics, currently including:
    * packet reordering
    * backpressure
* Live updated CLI output via the `notty` library.
* Enable/disable (two-way) bandwidth-monitoring per connection.
* Choose packet-size when bandwidth-monitoring.
  * Be aware that `UDP` datagrams should be around the size of the 
    [maximum transmission unit](https://en.wikipedia.org/wiki/Maximum_transmission_unit).
    Try out different values - packets bigger than `MTU` will be fragmented at the `IP` layer;
    where if a fragment is lost, the whole datagram is lost.
* Latency monitoring (calculated as half the roundtrip time).
* `UDP` statistics:
  * lost packets 
  * packets out of order
* Choose between several different CLI UIs - currently `notty` and `log` 
  (`log` doesn't show stats for now, just logs packets).
  * The UI's are fully abstracted away from the `conntest` protocol, so new ones 
    can easily be added.

### To do
* `UDP` support for resending lost packets, including lost ack's
* Show stats via alternative simple textual output. This is useful if you run your unikernel 
  via e.g. `albatross` which currently splits console output on newlines.
* Add structured data logging output. Usecases:
  * Plot the stats over time with another program after/while running `conntest`
  * Explicitly log all failed connections too, for the `nmap`-alternative usecase. 
    Currently the `notty` TUI only shows the active connections.

### Suggest features!
If you find this unikernel useful, but it's e.g. missing some stats that you are interested in - then make an issue/PR (: 

## Compiling

```
opam install 'mirage>=4.0'
git clone https://github.com/rand00/conntest
cd conntest
mirage configure -t <TARGET> -f mirage/config.ml && make depend && mirage build -f mirage/config.ml
```
Note that the current version of ocaml(v5.0.0) is not yet compatible with with mirage, so do well to switch to v4.14.1 using the commands below.
```
opam switch create mirage 4.14.1
```
We then update the new opam environment.
```
eval $(opam env --switch=mirage) 
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

#### Network setup

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
You can run this script by saving the contents to a file (e.g., `network_setup.sh`), making it executable (e.g., `chmod +x network_setup.sh`), and then executing it as a superuser (e.g., `sudo ./network_setup.sh`).
