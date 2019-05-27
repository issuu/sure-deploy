FROM ocaml/opam2:alpine-3.9-ocaml-4.07 as builder
ENV OPAMYES=1
COPY sure-deploy.opam /home/opam/sure-deploy/
RUN git -C /home/opam/opam-repository pull --quiet && \
  opam update > /dev/null && \
  opam pin --no-action add sure-deploy /home/opam/sure-deploy && \
  opam depext sure-deploy && \
  opam install --deps-only sure-deploy && \
  opam install ocamlformat.0.9
COPY dune-project .ocamlformat src /home/opam/sure-deploy/
RUN (cd /home/opam/sure-deploy; sudo chown -R opam .; opam exec -- dune build @fmt @install) && \
  opam install sure-deploy

FROM alpine:3.9
ENTRYPOINT ["/usr/local/bin/sure-deploy"]
WORKDIR /home/opam
RUN apk update && \
  apk add tzdata libffi libressl2.7-libcrypto libressl2.7-libssl && \
  cp /usr/share/zoneinfo/UTC /etc/localtime && \
  echo "UTC" > /etc/timezone && \
  adduser -D opam && \
  touch /home/opam/docker-compose.yml
USER opam
COPY --from=builder /home/opam/.opam/*/bin/sure-deploy /usr/local/bin/
