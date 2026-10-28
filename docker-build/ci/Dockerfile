FROM debian:trixie-20250929-slim
RUN apt-get update && apt-get install -y ca-certificates curl
RUN curl -L -o racket.sh https://download.racket-lang.org/releases/9.2/installers/racket-9.2-aarch64-linux-buster-cs.sh \
    && bash racket.sh --unix-style --dest /usr \
    && rm racket.sh \
    && raco pkg install -i --deps search-auto --batch aws base debug casemate graph http-easy http lens http-easy-lib racket-langserver remote-shell-lib
# TODO: auto-generate the deps from info.rkt
# COPY info.rkt /info.rkt
