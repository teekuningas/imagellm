import { Elm } from './src/Main.elm'
import './styles/main.css'

const flags = {}

// Start the Elm application.
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

