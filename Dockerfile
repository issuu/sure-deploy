FROM ocaml/opam:alpine-3.6_ocaml-4.05.0 as builder
COPY sure-deploy.opam /home/opam/sure-deploy/
RUN git -C /home/opam/opam-repository pull --quiet && \
  opam update && \
  opam pin --no-action add sure-deploy sure-deploy && \
  opam depext sure-deploy && \
  opam install --deps-only sure-deploy
COPY src /home/opam/sure-deploy/src
RUN opam install sure-deploy

FROM alpine:3.6
ENTRYPOINT ["/usr/local/bin/sure-deploy"]
WORKDIR /home/opam
RUN adduser -D opam
USER opam
COPY --from=builder /home/opam/.opam/*/bin/sure-deploy /usr/local/bin/
