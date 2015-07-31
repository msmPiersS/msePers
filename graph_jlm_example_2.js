// Create nodes...
let joe = new Node('person');
joe.set('name', 'Joe');

let minecraft = new Node('game');
minecraft.set('name', 'Minecraft');

// Create edge...
let likes = new Edge('likes');

// link 'em!
likes.link(joe, minecraft);

// add more nodes...
let notch = new Node('person', {name: 'Notch'});
let created = new Edge('created').link(notch, minecraft);