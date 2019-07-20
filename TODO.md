X Remove the sample, rely on "candidates".
X Mode the env variable names to the "true main"
X Construct the list of providers in the "true main"
X rework the network signature
X explicitly return the IP when up
X clean up internal t-do code
- perhaps use a stream of log messages?
- Add the Hetzner provider. 
    - Important: preserve the "location" of images when persisting and
      hydrathing them.
    - One difference with DO is that you can't specify a name when creating a
      snapshot. Workaround: use tags.
        - Servers *do* have names!
    - Annoying: getting image info lists the *datacenter*, but creating an
      image from snapshot involves a *location*.
        - Only the *location* is important!

