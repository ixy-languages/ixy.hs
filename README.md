# ixy.hs

ixy.hs is a Haskell rewrite of the [ixy](https://github.com/emmericp/ixy) userspace network driver.
It is designed to be readable, idiomatic Haskell (as far as possible).
It supports Intel 82599 10GbE NICs (`ixgbe` family of devices).

## Features

* currenty less than 1500 lines of Haskell for a working driver
* relatively simple to use API
* documented code

## Build instructions

Install [stack](https://haskellstack.org) by whatever means.

Make sure to `libpcre3-dev` is installed, otherwise execute (or equivalent):
```
apt install libpcre3-dev
```

Ixy.hs needs hugepages to work. To allocate some use the provided script `setup-hugetlbfs.sh`:
```
sudo ./setup-hugetlbfs.sh
```

Next step is to build ixy.hs with some compiler optimizations, to get extra juice:
```
stack build --ghc-options="-O2 -fllvm"
```

You can then go ahead and run the driver with (replace the PCI ids):
```
stack run 0000:00:00.0 0000:00:00.1 batchSize
```

You can choose any of these batch sizes (go with 128): 1,2,4,8,16,32,64,128,256

The build binaries can be found in
`.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.2.0.1/build/forwarder/forwarder`.

### API

`src/Lib.hs` defines ixy.hs's public API.

### Docs

Docs can be generated with the command `stack haddock --haddock-internal`

## License

ixy.hs is licensed under the MIT license.

## Disclaimer

ixy.hs is not production-ready.
Do not use it in critical environments.
DMA may corrupt memory.

## Other languages

Check out the [other ixy implementations](https://github.com/ixy-languages).

---

## Profiling
![Flamegraph](https://raw.githubusercontent.com/ixy-languages/ixy.hs/master/flame.svg?sanitize=true)
![Memgraph](https://raw.githubusercontent.com/ixy-languages/ixy.hs/master/mem.svg?sanitize=true)
