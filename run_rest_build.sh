#!/bin/bash
curl -v -X POST -d "{\"title\": \"test\", \"branch\": \"origin/master\", \"revision\": \"rev\", \"author\": \"Peter\", \"tags\": []}" http://localhost:8093/project/1/build

curl -v -X POST -d "{\"title\": \"test [ci skip]\", \"branch\": \"origin/master\", \"revision\": \"rev\", \"author\": \"Peter\", \"tags\": []}" http://localhost:8093/project/1/build

curl -v -X POST -d "{\"title\": \"test [ci skip]\", \"branch\": \"origin/master\", \"revision\": \"rev\", \"author\": \"Peter\", \"tags\": []}" http://localhost:8093/project/1/build
