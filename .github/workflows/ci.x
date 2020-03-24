name: Build it and test it
# This workflow is triggered on pushes to the repository.
on: 
  push:
    branches-ignore:
      - "feature/*"  # don't run build/test against a feature branch -- no SLA there
  pull_request:
    branches-ignore:
      - "feature/*"

jobs:
  build:
    name: Grand Build
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v1
        with:
          fetch-depth: 1
      - name: Set up JDK 11
        uses: actions/setup-java@v1
        with:
          java-version: '11.0.1'
      - name: Build and Test
        id: sbt
        uses: lokkju/github-action-sbt@master
        with:
          commands: test
      - name: Publish
        if: github.base_ref == 'master'
        id: release
        uses: lokkju/github-action-sbt@master
        env:
          BINTRAY_USER: ${{ secrets.BINTRAY_UID }}
          BINTRAY_PASS: ${{ secrets.BINTRAY_PWD }}
        with:
          commands: publish
