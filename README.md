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
Usage: csv2json blacklist &lt;blacklist file&gt;
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
$ ./csv2json customers data/customers.csv data/customers_originators.csv data/customers_users.csv 1,2,3,4,5,6,7,8 1,2,3,4,5,6,7,8 &gt; customers.json
$ mongoimport --host 127.0.0.1 --port 40001 --db kelly --collection customers --file customers.json -v
</pre>

<pre>
$ ./csv2json blacklist data/blacklist.csv &gt; blacklist.json
$ mongoimport --host 127.0.0.1 --port 27017 --db kelly --collection blacklist --file blacklist.json -v
</pre>
