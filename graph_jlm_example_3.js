// Add even more nodes
let mojang = new Node('company', {name: 'Mojang'});
let microsoft = new Node('company', {name: 'Microsoft'});
let jennifer = new Node('person', {name: 'Jennifer'});

new Edge('founded').link(notch, mojang);
new Edge('acquired').link(microsoft, mojang);
new Edge('purchased').link(jennifer, minecraft);
new Edge('prints_money_for').link(minecraft, microsoft);

/*

  Our new graph...

                    Jennifer
                       | (purchased)
                       v
  Joe --(likes)--> Minecraft <--(created)-- Notch
    (prints_money_for) |                      | (founded)
                       v                      v
                   Microsoft --(acquired)--> Mojang

*/