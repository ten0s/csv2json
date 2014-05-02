[![Build Status](https://travis-ci.org/ten0s/csv2json.svg?branch=master)](https://travis-ci.org/ten0s/csv2json)

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
$ ./csv2json
Usage: csv2json networks &lt;networks file&gt; &lt;prefixes file&gt;
Usage: csv2json networks_maps &lt;maps file&gt; &lt;maps to networks file&gt;
Usage: csv2json originators &lt;originators file&gt;
Usage: csv2json users &lt;users file&gt; &lt;des key&gt; &lt;ivec&gt;
Usage: csv2json customers &lt;customers file&gt;
Usage: csv2json customers &lt;customers file&gt; &lt;originators file&gt; &lt;users file&gt; &lt;des key&gt; &lt;ivec&gt;
</pre>

<pre>
$ ./csv2json networks data/networks.csv data/network_prefixes.csv &gt; networks.json
$ mongoimport --host 127.0.0.1 --port 27017 --db kelly --collection networks --file networks.json -v
</pre>

<pre>
$ ./csv2json network_maps data/network_maps.csv data/maps_to_networks.csv &gt; network_maps.json
$ mongoimport --host 127.0.0.1 --port 27017 --db kelly --collection network_maps --file network_maps.json -v
</pre>

<pre>
$ ./csv2json originators data/postpaid_customers_originators.csv &gt; postpaid_customers_originators.json
</pre>
