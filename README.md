## Prerequisites

In order to compile and run the **csv2json** utility you need to have [Erlang](http://www.erlang.org/) and GNU make installed.

## Compilation

<pre>
$ git clone https://github.com/ten0s/csv2json.git
$ cd csv2json
$ make
</pre>

## Usage

<pre>
$ ./csv2json networks data/pmm_networks.csv data/pmm_prefixes.csv > pmm_networks.json
$ mongoimport --host 127.0.0.1 --port 27017 --db kelly --collection networks --file pmm_networks.json -v
</pre>

<pre>
$ ./csv2json network_maps data/pmm_network_maps.csv data/pmm_maps_to_networks.csv > pmm_network_maps.json
$ mongoimport --host 127.0.0.1 --port 27017 --db kelly --collection network_maps --file pmm_network_maps.json -v
</pre>
