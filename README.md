# imagecube #

Application to project images on cubes.

[See some of my imagecubes on flickr](https://flic.kr/s/aHskVeDKWN)

If you want to create your own imagecubes go to this page

[http://entelijan.net/imagecube/](http://entelijan.net/imagecube/)

or (if you want to process larger images) downlodad the following jar

[imagecube.jar](https://github.com/wwagner4/imagecube/blob/master/bin/imagecube.jar?raw=true)

and start it using the following call

```shell
java -jar imagecube.jar
```

You may use the following options:

```shell
  -i, --inDir <value>   Input directory. Default is $HOME/imagecube/in.
  -o, --outDir <value>  Output directory. Default is $HOME/imagecube/out.
  -h, --handed <value>  Defines if the lashes are right or left handed. Values 'r' or 'l'. Default is 'r'.
  -c, --cutLines        Draw extra lines for cutting. Default is no extra lines. Set this option if your image(s) are very light.
  --help                Prints this usage text.
```


Of course you have to have [java](https://java.com) installed on your machine.

## Docker

Build the executable local using sbt;

```
sbt assembly
```

Build a docker image containig java and the newly builded jar file.

```
docker build -t wwagner4/solo:imagecube .
```

Push the dockerfile
```
docker login
docker push wwagner4/solo:imagecube
```

Pull and run the image on your server
```
docker pull wwagner4/solo:imagecube
docker run -p 8886:8090 wwagner4/solo:imagecube & 
docker run -it --rm -p 8886:8090 wwagner4/solo:imagecube

http://37.252.189.71:8886
http://localhost:8886
```

