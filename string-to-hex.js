
import { compose } from './compose';

const toUpperCase = (message) => message.toUpperCase();

function identity(message) {
    console.log(message)
    return message;
}

function asciiToHex(str) {
    var arr = [];
    for (var n = 0, l = str.length; n < l; n++) {
        var hex = Number(str.charCodeAt(n)).toString(16);
        arr.push(hex);
    }
    return arr.join('');
}

function encoder(message) {
    return compose(
        // identity,º
        asciiToHex,
        // identity,
        toUpperCase)
        (message)
}