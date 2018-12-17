FROM ocaml/opam2:alpine-3.8-ocaml-4.07 as builder
ENV OPAMYES=1
COPY sure-deploy.opam /home/opam/sure-deploy/
RUN git -C /home/opam/opam-repository pull --quiet && \
  opam update > /dev/null && \
  opam pin --no-action add sure-deploy /home/opam/sure-deploy && \
  opam depext sure-deploy && \
  opam install --deps-only sure-deploy
COPY src /home/opam/sure-deploy/src
RUN opam install sure-deploy

FROM alpine:3.8
ENTRYPOINT ["/usr/local/bin/sure-deploy"]
WORKDIR /home/opam
RUN apk update && \
  apk add tzdata && \
  adduser -D opam && \
  touch /home/opam/docker-compose.yml
USER opam
COPY --from=builder /home/opam/.opam/*/bin/sure-deploy /usr/local/bin/
