"use strict";


exports.newResponseImpl = function newResponseImpl (b) {
    return new Response(b);
};

exports.parseJsonImpl = function parseJsonImpl (r) {
    return r.json();
};
