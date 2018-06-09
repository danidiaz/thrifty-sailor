# thrifty-sailor

## What's this?

A command-line tool for taking a snapshot of a Digital Ocean droplet and
destroying the droplet afterwards. Also works in reverse direction: restore a
droplet from a snapshot and destroy the snapshot afterwards.

## Why do that?

For saving money, when you don't need to have a droplet active all the time.

At the time of writing this, the cheapest droplet—with 25 GB disk space—costs
$5/mo. It will be billed even when it is off.

Meanwhile, snapshot cost is based on space used, and it's charged at a rate of
$0.05/GB/mo. Even if you use the full 25 GB disk, that will be $1.25/mo.

So, if you have a pet development droplet, it is cheaper to keep it as a
snapshot while you are not using it. But the cycle of snapshotting, deleting
the droplet, restoring the droplet and deleting the snapshot is tedious to
perform through the web interface.

## Can't I do that with doctl already?

Yes. I just reinvented the wheel for educational purposes.

Here's a [doctl tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-doctl-the-official-digitalocean-command-line-client).

Here are some doctl example commands:

    doctl compute droplet list
    doctl compute droplet get 11111111
    doctl compute droplet-action snapshot 11111111 --snapshot-name lochlomond-snapshot
    doctl compute droplet create lochlomond --size 1gb --image 11111111 --region fra1
    doctl compute snapshot list
    doctl compute snapshot get 11111111 --verbose
    doctl compute snapshot delete 11111111 --force

## Ok, I want to use this anyway, how to build & install it?

You'll need [cabal-install >= 2.0](https://www.haskell.org/cabal/download.html).

--TBD

## How to configure it, once installed?

You must have a Digital Ocean token in an environment variable.

First, execute:

    thrifty-sailor example

It will spit out an example JSON configuration file in stdout. Copy it to 

    $HOME/.config/thrifty-sailor/config.json 
    
As the value of `token_environment_variable` put the name of the environment
variable holding the Digital Ocean token.

Then run

    thrifty-sailor status

A list of your droplets and snapshots will appear on stdout. Select the one you
want to target and write its "droplet_name", "region_slug" and "size_slug" in
the configuration file.

Finally, in the "snapshot_name" of the configuration file, write the name you
want to give to the snapshot that will be generated.

Having done this, invoking

    thrifty-sailor down

will shut down the droplet, snapshot it, and delete the droplet.

And invoking

    thrifty-sailor up 

will restore the snapshot, wait until the droplet is active, and delete the
snapshot.

