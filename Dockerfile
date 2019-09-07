FROM openjdk:11

COPY web/target/scala-2.12/imagecube-web.jar /jar/

EXPOSE 8090

CMD java -jar /jar/imagecube-web.jar


