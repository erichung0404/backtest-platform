// We don't yet have an API to know when an element is updated, so we'll poll
// and if we find the content has changed, we'll scroll down to show the new
// comments.
window.setInterval(function() {
  var verbose = document.getElementById('text');
  scrollToBottom2();
}, 0);

// Scroll to the bottom of the window.
function scrollToBottom2(){
  var verbose = document.getElementById('text');
  verbose.scrollTop = verbose.scrollHeight;
}

// For WFA
window.setInterval(function() {
  var verbose2 = document.getElementById('WFAtext');
  scrollToBottom3();
}, 0);

// Scroll to the bottom of the window.
function scrollToBottom3(){
  var verbose2 = document.getElementById('WFAtext');
  verbose2.scrollTop = verbose2.scrollHeight;
}