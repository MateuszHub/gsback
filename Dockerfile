FROM adoptopenjdk/openjdk11:x86_64-ubuntu-jdk-11.0.16.1_1-slim

WORKDIR /app

COPY . /app

RUN \
    apt-get update && \
    apt-get install -y gnupg && \
    apt-get install -y curl && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    apt-get install -y sbt && \
    sbt sbtVersion

CMD ["sbt", "run", "-Dhttps.port=9443"]


