# thrifty-sailor

## Build instructions

    cabal new-build

## Digital Ocean API

[API V2](https://developers.digitalocean.com/documentation/v2/)

# Alternative: doctl

[doctl tutorial](https://www.digitalocean.com/community/tutorials/how-to-use-doctl-the-official-digitalocean-command-line-client)

doctl examples:

    doctl compute droplet list
    doctl compute droplet get 11111111
    doctl compute droplet-action snapshot 11111111 --snapshot-name lochlomond-snapshot
    doctl compute droplet create lochlomond --size 1gb --image 11111111 --region fra1
    doctl compute snapshot list
    doctl compute snapshot get 11111111 --verbose
    doctl compute snapshot delete 11111111 --force
