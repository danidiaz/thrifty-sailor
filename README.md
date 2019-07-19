# thrifty-sailor

## What's this?

A command-line tool for taking a snapshot of a Digital Ocean droplet and
destroying the droplet afterwards. Also works in reverse direction: restore a
droplet from a snapshot and destroy the snapshot afterwards.

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

TODO: update this

You must have a Digital Ocean token in an environment variable.

First, execute:

    thrifty-sailor example

It will spit out an example JSON configuration file in stdout. Copy it to 

    $HOME/.config/thrifty-sailor/config.json 
    
As the value of `token_environment_variable` write the name of the environment
variable holding the Digital Ocean token.

Then run

    thrifty-sailor status

A list of your droplets and snapshots will appear on stdout. Select the droplet
you want to target and write its "name", "region_slug" and "size_slug" in the
configuration file.

Finally, in the "snapshot_name" entry of the configuration file, write the name
you want to give to the snapshot that will be generated.

Having done this, invoking

    thrifty-sailor down

will shut down the target droplet, snapshot it, and delete the droplet.

And invoking

    thrifty-sailor up 

will restore the droplet from the snapshot, wait until the droplet is active,
and delete the snapshot.

