import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.download.subscribe(function([filename, contentType, contents]) {
  const data = encodeURI(`data:${contentType},${contents}`);
  const link = document.createElement('a');
  link.setAttribute('href', data);
  link.setAttribute('download', filename);
  link.click();
});

registerServiceWorker();
