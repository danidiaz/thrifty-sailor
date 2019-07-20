# thrifty-sailor

## What's this?

A command-line tool for taking a snapshot of a cloud provider (Digital Ocean or
Hetzner) server and destroying the droplet afterwards. Also works in reverse
direction: restore a server from a snapshot and destroy the snapshot
afterwards. 

## Why do that?

For saving money, when you don't need to have a droplet active all the time.

In many cloud providers, snapshots are cheaper than running—or even
stopped—servers. So, if you have a pet development server, it's cheaper to keep
it as a snapshot while you aren't using it.

But the cycle of snapshotting, deleting the server, restoring the server and
deleting the snapshot is tedious to perform through the web interface.

### DO

At the time of writing this, the cheapest droplet—with 1GB RAM, 25 GB
disk—costs $5/mo. It will be billed even when it is off.

Meanwhile, snapshot cost is based on space used, and it's charged at a rate of
[$0.05/GB/mo](https://www.digitalocean.com/community/questions/a-snapshot-will-be-full-charged-when-created-or-will-be-per-hour-like-other-services).
Even if you use the full 25 GB disk, that will be $1.25/mo.

### Hetzner

At the time of writing this, the cheapest droplet—with 2GB RAM, 20 GB disk—costs
3€/mo. It will be billed even when it is off.

Meanwhile, snapshot cost is based on space used, and it's charged at a rate of
$0.01/GB/mo.

## Can't I do that with each cloud provider's command-line tools already?

Yes. I just reinvented the wheel for educational purposes.

### DO

Here's a [doctl tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-doctl-the-official-digitalocean-command-line-client).

Here are some doctl example commands:

    doctl compute droplet list
    doctl compute droplet get 11111111
    doctl compute droplet-action snapshot 11111111 --snapshot-name lochlomond-snapshot
    doctl compute droplet create lochlomond --size 1gb --image 11111111 --region fra1
    doctl compute snapshot list
    doctl compute snapshot get 11111111 --verbose
    doctl compute snapshot delete 11111111 --force

### Hetzner

See the [hcloud documentation](https://github.com/hetznercloud/cli).

## Ok, I want to use this anyway, how to build & install it?

You'll need [GHC](https://www.haskell.org/ghc/download.html) and [cabal-install >= 2.2](https://www.haskell.org/cabal/download.html).

Inside the project folder, run

    cabal v2-install --symlink-bindir=<targetpath>

## How to configure it, once installed?

Assume you have a running vps that you want to be managed by thrifty.

You'll need to define one or both of these environment variables, with the
corresponding cloud provider token as value:

    - For Digital Ocean: DIGITALOCEAN_ACCESS_TOKEN
    - For Hetzner: HCLOUD_TOKEN 

Then in the file 

    $HOME/.config/thrifty-sailor/config.json 

write the following config skeleton:

```
{
    "do" : {
    },
    "hetzner" : {
    }
}
```

We will put information about server-snapshot pairs there.


### Digital Ocean

For Digital Ocean, run `thrifty candidates do`. A JSON array of possible entries
will appear on stdout. Copy one of the entries under the "do" object, giving it a name,
for example "foo":

```
{
    "do" : {
        "foo" : {"droplet":{"size_slug":"s-1vcpu-1gb","region_slug":"ams3","name":"somedropletname"},"snapshot_name":"somedropletname_snapshot"}
    }
}
```

You can change the `snapshot_name` at this point, if you want.

Having done this, running

    thrifty down do foo

will shut down the target droplet, snapshot it, and **delete** the droplet.

Running

    thrifty up do foo 

will restore the droplet from the snapshot, wait until the droplet is active,
and delete the snapshot.

### Hetzner


For Hetzner, run `thrifty candidates hetzner`. A JSON array of possible entries
will appear on stdout. Copy one of the entries under the "hezner" object, giving it a name,
for example "foo":

```
{
    "hetzner" : {
        "foo" : {"server":{"server_type":"cx11","server_name":"somedropletname","server_location":"nbg1"}, "snapshot_label_value":"somedropletname_snapshot"}
    }
}
```

You can change the `snapshot_label_value` at this point, if you want.

Having done this, running

    thrifty down hetzner foo

will shut down the target server, snapshot it, and **delete** the server.

Running

    thrifty up hetzner foo 

will restore the server from the snapshot, wait until the server is active,
and delete the snapshot.

