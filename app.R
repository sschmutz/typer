library(shiny)
library(bslib)
library(fontawesome)

# List of sentences to be typed
sentences <- c(
  "The quick brown fox jumps over the lazy dog.",
  "Pack my box with five dozen liquor jugs.",
  "How vexingly quick daft zebras jump!",
  "Sphinx of black quartz, judge my vow.",
  "Two driven jocks help fax my big quiz."
)

ui <- page_fluid(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #2C2C2C;
        font-family: Arial, sans-serif;
        color: #E0E0E0;
      }
      #typing-area {
        max-width: 600px;
        margin: 50px auto;
        padding: 20px;
      }
      #target-text, #text-wrapper {
        font-size: 18px;
        margin-bottom: 20px;
        font-family: monospace;
      }
      #target-text {
        color: #B0B0B0;
        font-weight: normal;
        margin-top: 50px;
      }
      #text-wrapper {
        position: relative;
      }
      #interactive-text, #highlighted-text {
        width: 100%;
        padding: 10px 0;
        outline: none;
        background-color: transparent;
        font-size: 18px;
        font-family: monospace;
        line-height: 1.5;
        overflow: hidden;
        resize: none;
        font-weight: 450;
        color: #F0F0F0;
      }
      #interactive-text {
        border: none;
        position: relative;
        z-index: 1;
        caret-color: transparent;
      }
      #highlighted-text {
        position: absolute;
        top: 0;
        left: 0;
        pointer-events: none;
        white-space: pre-wrap;
        word-wrap: break-word;
        border: none;
        z-index: 0;
      }
      .error {
        background-color: #FF4C4C;
        color: #2C2C2C;
      }
      @keyframes blink {
        0% { opacity: 1; }
        50% { opacity: 0; }
        100% { opacity: 1; }
      }
      #text-wrapper::after {
        content: '|';
        color: #FFD700;
        font-weight: bold;
        font-size: 18px;
        position: absolute;
        top: 8px;
        left: -3px;
        opacity: 0;
      }
      #text-wrapper.focused::after {
        animation: blink 1s step-end infinite;
      }
    "))
  ),
  div(id = "typing-area",
    div(style = "height: 100px;"),
    uiOutput("target_text"),
    uiOutput("interactive_text")
  )
)

server <- function(input, output, session) {
  # Use reactiveVal for the target sentence
  target <- reactiveVal(sample(sentences, 1))
  
  output$target_text <- renderUI({
    tags$p(id = "target-text", 
           style = "font-size: 18px; font-family: monospace; margin-bottom: 20px; font-weight: normal;",
           target())
  })
  
  output$interactive_text <- renderUI({
    tagList(
      tags$div(
        id = "text-wrapper",
        tags$div(
          id = "highlighted-text",
          HTML(highlight_errors("", target()))
        ),
        tags$textarea(
          id = "interactive-text",
          oninput = "this.parentNode.querySelector('#highlighted-text').innerHTML = highlightErrors(this.value);
                     this.style.height = 'auto';
                     this.style.height = this.scrollHeight + 'px';
                     updateCursorPosition(this);",
          rows = 1,
          autocomplete = "off",
          spellcheck = "false"
        )
      ),
      tags$script(HTML(sprintf(
        "function highlightErrors(typed) {
          const target = '%s';
          const typedWords = typed.split(/\\s+/);
          const targetWords = target.split(/\\s+/);
          
          let result = '';
          
          for (let i = 0; i < typedWords.length; i++) {
            const typedWord = typedWords[i];
            const targetWord = i < targetWords.length ? targetWords[i] : '';
            
            for (let j = 0; j < typedWord.length; j++) {
              if (j < targetWord.length) {
                if (typedWord[j] !== targetWord[j]) {
                  result += '<span class=\"error\">' + typedWord[j] + '</span>';
                } else {
                  result += typedWord[j];
                }
              } else {
                result += '<span class=\"error\">' + typedWord[j] + '</span>';
              }
            }
            
            if (i < typedWords.length - 1) {
              result += ' ';
            }
          }
          
          return result;
        }

        function updateCursorPosition(textarea) {
          const wrapper = textarea.parentNode;
          const cursorPosition = textarea.selectionStart;
          const text = textarea.value.substring(0, cursorPosition);
          const textWidth = getTextWidth(text, getComputedStyle(textarea).font);
          wrapper.style.setProperty('--cursor-left', `${textWidth - 3}px`);
        }

        function getTextWidth(text, font) {
          const canvas = getTextWidth.canvas || (getTextWidth.canvas = document.createElement('canvas'));
          const context = canvas.getContext('2d');
          context.font = font;
          return context.measureText(text).width;
        }

        function focusTextarea() {
          const textarea = document.getElementById('interactive-text');
          textarea.focus();
          updateCursorPosition(textarea);
        }

        document.addEventListener('DOMContentLoaded', focusTextarea);
        document.addEventListener('click', focusTextarea);
        document.addEventListener('keydown', function(event) {
          if (!event.ctrlKey && !event.altKey && !event.metaKey && event.key.length === 1) {
            focusTextarea();
          }
        });

        const textWrapper = document.getElementById('text-wrapper');
        const interactiveText = document.getElementById('interactive-text');

        interactiveText.addEventListener('focus', function() {
          textWrapper.classList.add('focused');
          setTimeout(updateCursorPosition, 0, this);
        });

        interactiveText.addEventListener('blur', function() {
          textWrapper.classList.remove('focused');
        });

        // Prevent linebreaks
        interactiveText.addEventListener('keydown', function(event) {
          if (event.key === 'Enter') {
            event.preventDefault();
          }
        });
      ", target()
      ))),
      tags$style(HTML("
        #text-wrapper::after {
          left: var(--cursor-left, -3px);
        }
      "))
    )
  })
}

highlight_errors <- function(typed, target) {
  typed_words <- strsplit(typed, "\\s+")[[1]]
  target_words <- strsplit(target, "\\s+")[[1]]
  
  result <- character(0)
  
  for (i in seq_along(typed_words)) {
    typed_word <- typed_words[i]
    target_word <- if (i <= length(target_words)) target_words[i] else ""
    
    word_result <- character(nchar(typed_word))
    
    for (j in 1:nchar(typed_word)) {
      if (j <= nchar(target_word)) {
        if (substr(typed_word, j, j) != substr(target_word, j, j)) {
          word_result[j] <- sprintf('<span class="error">%s</span>', substr(typed_word, j, j))
        } else {
          word_result[j] <- substr(typed_word, j, j)
        }
      } else {
        word_result[j] <- sprintf('<span class="error">%s</span>', substr(typed_word, j, j))
      }
    }
    
    result <- c(result, paste(word_result, collapse = ""))
    
    if (i < length(typed_words)) {
      result <- c(result, " ")
    }
  }
  
  paste(result, collapse = "")
}

shinyApp(ui, server)
