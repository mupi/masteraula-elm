require( './styles/main.scss' );
// inject bundled Elm app into div#main
var Elm = require( '../Main' );
var storedState = localStorage.getItem('elm-app-save');
var startingState = storedState ? JSON.parse(storedState) : null;
var mupiApp = Elm.Main.fullscreen(startingState);
mupiApp.ports.setLocalStorage.subscribe(function(state) {
    localStorage.setItem('elm-app-save', JSON.stringify(state));
});
