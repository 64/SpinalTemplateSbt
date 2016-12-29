Spinal Base Project
============
This repository is a base SBT project added to help non Scala/SBT native people in their first steps.

## Basics, without any IDE

You need to install Java JDK and SBT

```sh
sudo apt-get install openjdk-7-jdk

echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt-get update
sudo apt-get install sbt
```

Clone or download this repository.

```sh
git clone https://github.com/SpinalHDL/SpinalBaseProject.git
```

Open a terminal in the root of it and run "sbt run". At the first execution, the process could take some seconds

```sh
cd SpinalBaseProject
sbt run
```

Normally, this "sbt run" command must generate an output files named MyTopLevel.vhd.
The top level spinal code is defined into src\main\scala\MyCode

## Basics, with Intellij IDEA and its scala plugin

You need to install :

- Java JDK
- Scala
- SBT
- Intellij IDEA 14.1.3 (the free Community Edition is nice)
- Intellij IDEA Scala plugin

And do the following :

- Clone or download this repository.
- In Intellij IDEA, "import project" with the root of this repository, Import project from external model SBT, Check all box
- In addition maybe you need to specify some path like JDK to Intellij
- In the project (Intellij project GUI), right click on src/main/scala/MyCode/TopLeve.scala and select "Run MyTopLevel"

Normally, this must generate output files MyTopLevel.vhd.

## Basics, with Eclipse and its scala plugin

You need to install :

- Java JDK
- Scala
- SBT
- Eclipse (tested with Mars.2 - 4.5.2)
- [scala plugin](http://scala-ide.org/) (tested with 4.4.1)

And do the following :

- Clone or download this repository.
- Run ```sbt eclipse``` in the ```SpinalBaseProject``` directory.
- Import the eclipse project from eclipse.
- In the project (eclips project GUI), right click on src/main/scala/MyCode/TopLeve.scala and select "Run as" > "Scala application"

Normally, this must generate output file ```MyTopLevel.vhd```.

