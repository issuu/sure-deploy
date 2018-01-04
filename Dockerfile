FROM ocaml/opam:debian-9_ocaml-4.05.0 as builder
COPY sure-deploy.opam /home/opam/sure-deploy/
RUN git -C /home/opam/opam-repository pull --quiet && \
  opam update && \
  opam pin --no-action add sure-deploy sure-deploy && \
  (opam depext -ln sure-deploy | grep -oP -- '- \K.+' > depexts) && \
  xargs sudo apt-get install -y --no-install-recommends < depexts && \
  opam install --deps-only sure-deploy
COPY src /home/opam/sure-deploy/src
RUN opam install sure-deploy

FROM debian:9
ENTRYPOINT ["/usr/local/bin/sure-deploy"]
RUN apt-get update && \
  apt-get upgrade -y && \
  useradd -ms /bin/bash opam
WORKDIR /home/opam
COPY --from=builder /home/opam/depexts depexts
RUN xargs apt-get install --no-install-recommends -y < depexts
USER opam
COPY --from=builder /home/opam/.opam/*/bin/sure-deploy /usr/local/bin/
