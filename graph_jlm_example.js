// all from https://medium.com/@keithwhor/using-graph-theory-to-build-a-simple-recommendation-engine-in-javascript-ec43394b35a3
let joe = {type: 'node', properties: {name: 'joe'}, input: [], output: []};
let likes = {type: 'edge', properties: {name: 'likes'}, input: null, output: null};
let minecraft = {type: 'node', properties: {name: 'minecraft'}, input: [], output: []};

joe.output.push(likes);
likes.input = joe;
likes.output = minecraft;
minecraft.input.push(likes);