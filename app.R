library(shiny)
library(bslib)
library(readr)
library(here)
library(dplyr)

# Read sentences from CSV file
sentences <-
  read_csv(here("data", "sentences.csv")) |> 
  pull(sentence)

# Randomly choose one sentence
sentence <- sample(sentences, 1)

ui <- page_fluid(
  theme = bs_theme(bg = "#1E2A38", fg = "#FFFFFF"),
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      body {
        font-family: 'Source Sans Pro', sans-serif;
      }
      #sentence {
        font-size: 24px;
        letter-spacing: 0.5px;
        line-height: 1.5;
        text-align: justify;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 80%;
        max-width: 800px;
      }
      .default {
        color: #657281;
      }
      .correct {
        color: #D1E0EF;
      }
      .incorrect {
        color: #EE6431;
      }
      #reloadButton {
        position: absolute;
        top: 60%;
        left: 50%;
        transform: translateX(-50%);
        background-color: transparent;
        border: none;
        cursor: pointer;
        transition: transform 0.3s;
        outline: none;
      }
      #reloadButton:hover {
        transform: translateX(-50%) scale(1.1);
      }
      #reloadButton svg {
        fill: #657281;
        transition: fill 0.3s;
      }
      #reloadButton:hover svg {
        fill: #D1E0EF;
      }
      #githubLink {
        position: absolute;
        top: 10px;
        right: 10px;
      }
      #githubLink svg {
        fill: #657281;
        transition: fill 0.3s;
      }
      #githubLink:hover svg {
        fill: #D1E0EF;
      }
    "))
  ),
  tags$div(id = "sentence"),
  tags$button(
    id = "reloadButton",
    title = "Reload Sentence",
    HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24"><path d="M17.65 6.35C16.2 4.9 14.21 4 12 4c-4.42 0-7.99 3.58-7.99 8s3.57 8 7.99 8c3.73 0 6.84-2.55 7.73-6h-2.08c-.82 2.33-3.04 4-5.65 4-3.31 0-6-2.69-6-6s2.69-6 6-6c1.66 0 3.14.69 4.22 1.78L13 11h7V4l-2.35 2.35z"/></svg>')
  ),
  tags$a(
    href = "https://github.com/sschmutz/typer",
    target = "_blank",
    id = "githubLink",
    title = "View on GitHub",
    HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24"><path d="M12 0c-6.626 0-12 5.373-12 12 0 5.302 3.438 9.8 8.207 11.387.599.111.793-.261.793-.577v-2.234c-3.338.726-4.033-1.416-4.033-1.416-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23.957-.266 1.983-.399 3.003-.404 1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222v3.293c0 .319.192.694.801.576 4.765-1.589 8.199-6.086 8.199-11.386 0-6.627-5.373-12-12-12z"/></svg>')
  ),
  tags$script(HTML("
    let sentences = [];
    let sentence = '';
    let currentIndex = 0;

    function getRandomSentence() {
      return sentences[Math.floor(Math.random() * sentences.length)];
    }

    function initializeSentence() {
      const sentenceElement = document.getElementById('sentence');
      sentenceElement.innerHTML = '';
      sentence.split('').forEach(char => {
        const span = document.createElement('span');
        span.textContent = char;
        span.className = 'default';
        sentenceElement.appendChild(span);
      });
      currentIndex = 0;
    }

    function reloadSentence() {
      sentence = getRandomSentence();
      initializeSentence();
    }

    document.addEventListener('keydown', function(event) {
      const spans = document.querySelectorAll('#sentence span');
      
      if (event.key === 'Backspace' && currentIndex > 0) {
        currentIndex--;
        spans[currentIndex].className = 'default';
      } else if (currentIndex < sentence.length) {
        const char = sentence[currentIndex].toLowerCase();
        
        if (event.key.toLowerCase() === char) {
          spans[currentIndex].className = 'correct';
          currentIndex++;
        } else if (event.key.length === 1) {
          spans[currentIndex].className = 'incorrect';
          currentIndex++;
        }
      }

      // Prevent default behavior for space key
      if (event.key === ' ') {
        event.preventDefault();
      }
    });

    document.addEventListener('visibilitychange', function() {
      if (!document.hidden) {
        reloadSentence();
      }
    });

    document.getElementById('reloadButton').addEventListener('click', reloadSentence);

    Shiny.addCustomMessageHandler('initializeSentence', function(data) {
      sentences = data.sentences;
      sentence = getRandomSentence();
      initializeSentence();
    });
  "))
)

server <- function(input, output, session) {
  observe({
    session$sendCustomMessage("initializeSentence", list(sentences = sentences))
  })
}

shinyApp(ui, server)
