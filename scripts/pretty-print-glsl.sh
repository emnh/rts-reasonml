#echo $(./node_modules/.bin/glslx --format=json --pretty-print --keep-symbols --renaming=none test.glsl | jq '.shaders[0].contents') >| test2.glsl
./node_modules/.bin/glslx --format=json --pretty-print --keep-symbols --renaming=none --disable-rewriting test3.glsl |
jq '.shaders[0].contents' |
sed 's/\\n/¤/g' |
tr '¤' '\n' >| test4.glsl
