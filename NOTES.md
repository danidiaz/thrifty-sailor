# NOTES

## Build / Devel

    ghcid --command="cabal v2-repl exe:thrifty"

    ghcid --command="cabal v2-repl lib:thrifty"

    ghcid --command="cabal v2-repl lib:delays"

    ghcid --command="cabal v2-repl lib:prelude"

ghcid and repls can be opened directly in Vim's :term

    :below terminal ++rows=10 ghcid --command="cabal v2-repl lib:delays"

    :below terminal ++rows=10 ghcid --command="cabal v2-repl exe:thrifty"

    :below terminal ++close ++rows=10 cabal new-repl exe:thrifty
 
To actually build and run:

    cabal new-build

    cabal new-run exe:thrifty -- status

    cabal new-run exe:thrifty -- down

## Digital Ocean API

[API V2](https://developers.digitalocean.com/documentation/v2/)

[Snapshots and regions.](https://www.digitalocean.com/community/tutorials/how-to-migrate-digitalocean-droplets-using-snapshots#step-2-%E2%80%94-adding-the-snapshot-to-new-region-(optional))

It seems that the same snapshot can be present [in multiple regions](https://developers.digitalocean.com/documentation/v2/#list-all-droplet-snapshots):

> An array of the regions that the image is available in. The regions are
> represented by their identifying slug values.

I gather that [taking a new snapshot from a
droplet](https://developers.digitalocean.com/documentation/v2/#snapshot-a-droplet)
always happens in the *particular* region associated to the droplet?

And creating a droplet [requires you to specify a region](https://developers.digitalocean.com/documentation/v2/#create-a-new-droplet).

We can name snapshots. Snapshots can have the same name as droplets.

Snapshot ids are strings, droplets ids are ints. Don't ask me why.

Droplet names need not be unique.

[Error Retries and Exponential Backoff in AWS](https://docs.aws.amazon.com/general/latest/gr/api-retries.html)

[Truncated exponential backoff](https://cloud.google.com/storage/docs/exponential-backoff)

[introduction to snapshots](https://www.digitalocean.com/community/tutorials/an-introduction-to-digitalocean-snapshots)

[The only difference between your original droplet and the droplet you create from it's snapshot should be it's assigned IP address.](https://www.digitalocean.com/community/questions/snapshot-and-re-creating-a-server-from-it)

[How To Connect To Your Droplet with SSH](https://www.digitalocean.com/community/tutorials/how-to-connect-to-your-droplet-with-ssh)

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

[What is the OAuth 2.0 Bearer Token exactly?](https://stackoverflow.com/questions/25838183/what-is-the-oauth-2-0-bearer-token-exactly/25843058)


