
FROM busybox
COPY target/x86_64-unknown-linux-musl/release/apidoctor .
RUN ["chmod", "+x", "/apidoctor"]
ENTRYPOINT [ "/apidoctor" ]
