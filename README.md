# sure-deploy

[![Build Status](https://travis-ci.org/issuu/sure-deploy.svg?branch=master)](https://travis-ci.org/issuu/sure-deploy)

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
by [docker swarm not supporting it][swarmdetach].

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

### `verify`

After a `docker swarm` deployment has converged it might have ended in one of
two states:

1. The deployment succeeded. Congratulations, you are good to go.
2. The deployment failed. This might be due to any number of things. Maybe the
   configuration was invalid, the containers could not be started or have been
   rolled back.

After a deployment has finished you have to check in which of those states your
deployment ended up being. Since you already have a `docker-compose.yml` you
used to deploy, you can use the `verify` command to check whether what is
deployed is what you expected it to be.

Currently it checks that your stack contains exactly the same services as your
Docker Compose file specifies and these services run exactly the versions that
your Compose file requires.

Simple usage:

```sh
sure-deploy verify --host <SWARMHOST> <STACKNAME>
```

Since Docker Compose files can use template variables like `$IMAGE_NAME` or
`${REVISION}`, you can pass these values as environment variables, in the exact
same fashion as to the `docker` command:

```sh
REVISION=cee7f68 sure-deploy verify --host <SWARMHOST> <STACKNAME>
```

By default the `docker-compose.yml` in the current directory is read, but this
can be overridden, check `verify --help` for a list of options.

When using it via Docker image, you need to mount your `docker-compose.yml` to
`/home/opam/docker-compose.yml`, so `sure-deploy` can read your
`docker-compose.yml` inside the Docker container.

```sh
docker run --rm -ti \
  --mount type=bind,src=$(pwd)/docker-compose.yml,dst=/home/opam/docker-compose.yml,readonly \
  -e <VARIABLE_IN_DOCKER_COMPOSE>=<VALUE> \
  leonidasfromxiv/sure-deploy:<BUILD_ID> \
  verify --host <SWARMHOST> <STACKNAME>
```

The command is complicated but this is due to how verbose mounting things from
the local file system to a Docker container is.

## Docker image

For people not wanting to build the binary themselves, there is a ready-made
Docker container:

```sh
docker run --rm -ti leonidasfromxiv/sure-deploy:13 --help
```

Will show the available subcommands and how to run them. The general pattern to
use the software is:

```sh
docker run --rm -ti leonidasfromxiv/sure-deploy:<BUILD_ID> <ARGUMENTS_TO_SURE_DEPLOY>
```

As such the `converge` example from above can be run through `docker run` as well:

```sh
docker run --rm -ti leonidasfromxiv/sure-deploy:<BUILD_ID> converge --host <SWARMHOST> <STACKNAME>
```

## License

Apache 2.0, see [LICENSE](LICENSE).

[swarmdetach]: https://github.com/docker/cli/issues/373
