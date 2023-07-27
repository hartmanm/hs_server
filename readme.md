# hs_server
A bash script builds a docker container with Haskell, mounts a directory, and runs a local webserver from the container with content pulled from the mounted files / directories and the project/src/routes file

- requires docker and sed
- only supports macOS and Linux

to build and launch:

```bash
git clone https://github.com/hartmanm/hs_server.git

cd hs_server/project

bash hs_server
```

- note if no parameter is passed, the repository directory will be mounted

to mount a different directory, pass its fullpath as a parameter:

```bash
bash hs_server <HOST_DIRECTORY_FULLPATH_TO_MOUNT>
```

example:

```bash
bash hs_server /Users/mac/Downloads/test
```
