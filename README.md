# imagecube #

Application to project images on cubes.

[See some of my imagecubes on flickr](https://flic.kr/s/aHskVeDKWN)

If you want to create your own imagecubes go to this page

[http://entelijan.net/imagecube/](http://entelijan.net/imagecube/)

or (if you want to process larger images) downlodad the following jar

[imagecube.jar](https://github.com/wwagner4/imagecube/blob/master/bin/imagecube.jar)

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
```


Of course you have to have [java](https://java.com) installed on your machine.