#!/bin/bash
curl -X POST -d "{\"title\": \"test\", \"branch\": \"origin/master\", \"revision\": \"rev\", \"author\": \"Peter\", \"tags\": []}" http://localhost:8093/project/1/build
