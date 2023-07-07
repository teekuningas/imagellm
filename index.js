import { Elm } from './src/Main.elm'
import './styles/main.css'

const apiAddress = process.env.API_ADDRESS;
const flags = {
  "apiAddress": apiAddress
};

// Start the Elm application.
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

