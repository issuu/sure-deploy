# sure-deploy

Like Sure Fisk but for deployments. Alternatively: What `docker stack deploy`
should've been.

## Build

`sure-deploy` tries to adhere as much to OCaml community standards as possible.
As such it uses `opam` files to contain the dependencies and uses `jbuilder` to
build the executable.

With OPAM 2 (recommended) it is simply a matter of creating a local switch in
the project folder which will pin the project description, install all
dependencies and build the source code:

```sh
opam switch create ./ 4.05.0
```

Building it with OPAM 1.2.x is possible as well but requires manual pinning.

## Usage

There is one binary, `sure-deploy` that supports `--help` which can tell you
what options are supported and which parameters they each require and support.

To get more log information the `--verbose` flag can be passed, which will
output more information on what the command is doing at the moment.

### `converge`

The `converge` subcommand implements what is similar to the `--detach=false`
option to `docker service` but for `docker stack`. Its creation was motivated
by (docker swarm not supporting it)[swarmdetach].

Calling `converge` will poll `docker swarm` for all services part of the
specified stack and terminate once all the detected services reach a terminal
state, be it a successful deployment or a failure. This alllows to wait for
approximately the shortest possible time for a deployment before it can be
determined whether the deployment succeeded or not. The other possibility would
be to wait for a fixed time that a deployment would take but picking a time too
short would lead to spurious failures where deployment took longer than
expected and picking a time too long would unnecessarily prolong the time a
build takes.

Note that `converge` does not wait forever, there is a configurable maximum
time a deployment may take. Exceeding this time will make the `converge`
command signal a failure.

A sample `converge` run can be done this way:

```sh
sure-deploy converge --host <SWARMHOST> <STACKNAME>
```

### `check`

After a `docker swarm` deployment has converged it might have ended in one of
two states:

1. The deployment succeeded. Congratulations, you are good to go.
2. The deployment failed. This might be due to any number of things. Maybe the
   configuration was invalid, the containers could not be started or have been
   rolled back.

After a deployment you have to check. The `check` subcommand does exactly this:
checks whether the deployment was successful. There are a number of different
ways to check, because the definition of successful is not as simple.

What `check` currently supports is to make sure all images run the same image,
passed on the command line. This is clearly not a comprehensive works-for-all
solution but works as a first step. A more comprehensive solution would read
the `docker-compose` YAML file and check the information contained here.

## Docker image

For people not wanting to build the binary themselves, there is a ready-made
Docker container:

```sh
docker run --rm -ti docker-registry.issuu.com/sure-deploy:7 --help
```

Will show the available subcommands and how to run them. The general pattern to
use the software is:

```sh
docker run --rm -ti docker-registry.issuu.com/sure-deploy:<BUILD_ID> <ARGUMENTS_TO_SURE_DEPLOY>
```

As such the `converge` example from above can be run through `docker run` as well:

```sh
docker run --rm -ti docker-registry.issuu.com/sure-deploy:<BUILD_ID> converge --host <SWARMHOST> <STACKNAME>
```

## License

TODO

[swarmdetach]: https://github.com/docker/cli/issues/373
