#! /bin/sh
cd stanford && java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -quiet -port $1 -timeout 15000 -maxCharLength 300000
# cd stanford && java -mx4g -cp "*" edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port $1 -timeout 15000 1> /dev/null
