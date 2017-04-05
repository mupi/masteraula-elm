require( './styles/main.scss' );
// inject bundled Elm app into div#main
var Elm = require( '../Main' );
var storedState = localStorage.getItem('elm-app-save');
var startingState = storedState ? JSON.parse(storedState) : null;

var mupiApp = Elm.Main.fullscreen(startingState);

mupiApp.ports.setLocalStorage.subscribe(function(state) {
  localStorage.setItem('elm-app-save', JSON.stringify(state));
});

mupiApp.ports.displayDialog.subscribe(function(theDialog) {
  try {
      var dialog = document.getElementById(theDialog);
      if (!dialog) {
          console.log('Cannot display dialog: No dialog element. Use `Dialog.view` to construct one.');
          return;
      }
      if (!dialog.showModal) {
          if (typeof dialogPolyfill !== 'undefined' && dialogPolyfill.registerDialog) {
              dialogPolyfill.registerDialog(dialog);
          } else {
              console.log('Cannot display dialog: Your browser does not support the <dialog> element. Get a polyfill at:\\n\\nhttps://github.com/GoogleChrome/dialog-polyfill\\n');
              return;
          }
      }
      dialog.showModal();
  } catch (e) {
      console.log("A dialog method threw an exception. This is not supposed to happen; likely you're using a broken polyfill. If not, please file an issue:\\n\\nhttps://github.com/debois/elm-mdl/issues/new");
  }
});
