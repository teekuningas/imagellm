import { Elm } from './src/Main.elm'
import './styles/main.css'

const runtimeApiAddress = "%%RUNTIME_API_ADDRESS%%";

const flags = {
  "apiAddress": !runtimeApiAddress.includes("RUNTIME_API_ADDRESS") ? runtimeApiAddress : "http://localhost:8000"
};

// Start the Elm application.
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

