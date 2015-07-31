let users = getUsers();         // abstract function to get user data (i.e. SQL)
let listings = getListings();   // ... listings
let views = getViews();         // ... etc.
let favorites = getFavorites();
let requests = getRequests();

// quick and dirty O(n) function to get a node by id
function getNodeById(nodes, id) {
  return nodes.filter(function(node) {
    return node.get('id') === id;
  })[0];
}

users = users.map(function(user) {
  return new Node('user', user);
});

listings = listings.map(function(listing) {
  return new Node('listing', listing);
});

views = views.map(function(view) {
  return new Edge('view')
    .link(getNodeById(users, view.user_id), getNodeById(listings, view.listing_id));
});

favorites = favorites.map(function(favorite) {
  return new Edge('favorite')
    .link(getNodeById(users, favorite.user_id), getNodeById(listings, favorite.listing_id));
});

requests = requests.map(function(request) {
  return new Edge('request')
    .link(getNodeById(users, request.user_id), getNodeById(listings, request.listing_id));
});