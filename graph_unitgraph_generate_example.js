let ug = require('ug');

let graph = new ug.Graph();

// fetch data

let users = getUsers();         // abstract function to get user data (i.e. SQL)
let listings = getListings();   // ... listings
let views = getViews();         // ... etc.
let favorites = getFavorites();
let requests = getRequests();

// Add to graph

users.forEach(function(user) {
  graph.createNode('user', user);
});

listings.forEach(function(listing) {
  graph.createNode('listing', listing);
});

views.forEach(function(view) {
  graph.createEdge('view').link(
    graph.nodes('user').find(view.user_id),
    graph.nodes('listing').find(view.listing_id)
  ).setDistance(4);
});

favorites.forEach(function(favorite) {
  graph.createEdge('favorite').link(
    graph.nodes('user').find(favorite.user_id),
    graph.nodes('listing').find(favorite.listing_id)
  ).setDistance(2);
});

requests.forEach(function(request) {
  graph.createEdge('request').link(
    graph.nodes('user').find(request.user_id),
    graph.nodes('listing').find(request.listing_id)
  ).setDistance(1);
});

// save graph
graph.save('/path_to_saved_graph.ugd', function() {
  
  doAfterSave(); // do whatever you'd like.
  
});