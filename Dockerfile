FROM openjdk:11

COPY bin/imagecube-web.jar /jar/

EXPOSE 8090

CMD java -jar /jar/imagecube-web.jar


