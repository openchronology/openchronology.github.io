"use strict";


exports.newResponseImpl = function newResponseImpl (b) {
    return new Response(b);
};

exports.getArrayBufferImpl = function getArrayBufferImpl (r) {
    return r.arrayBuffer();
};
