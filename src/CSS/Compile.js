/* global exports */
"use strict";

// module CSS.Compile


exports.addStyleSheet = function(s){
  return function () {
    var e = document.createElement('style');
    e.appendChild(document.createTextNode(s));
    document.head.appendChild(e);
  };
}
