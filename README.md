# 🌻 🐝 Functional Virtual Machine 🐝 🌻

## Before you begin

### Installing Elm

`dbee` is written in Elm.
Elm is usually for front-end development,
but since it compiles to Javascript,
we want to run it in Node as a server application.

Follow the official guide to
[install Elm](https://guide.elm-lang.org/install/elm.html).
It's very easy, there are installers/instructions for Mac, Linux and Windows.

Then, check your Elm installation.

```sh
elm --version
```

### Installing Node

We only need Node to run the Javascript app as a server.

Follow the instructions to
[install nvm](https://github.com/nvm-sh/nvm#installing-and-updating).

```sh
# Verify you have nvm.
nvm --version

# Install the latest version of node.
nvm install node

# Make sure you are pointed to nvm's node version.
which node
# This could look something like:
#   /Users/<USER>/.nvm/versions/node/v14.4.0/bin/node
```

## Contributing

Contributions are welcome! For instructions on how to contribute,
please check the [Contribution](CONTRIBUTING.md) guide.
