ChatGPT
=======

https://docs.kanaries.net/es/topics/ChatGPT/how-to-use-chatgpt-api-python

.. code:: Bash

   from openai import OpenAI

   client = OpenAI(api_key='leo')
   import os
   import pandas as pd
   import time


   def get_completion(prompt, model="gpt-3.5-turbo"):
    messages = [{"role": "user", "content": prompt}]
    response = client.chat.completions.create(model=model,
    messages=messages,
    temperature=0)
    return response.choices[0].message.content


   prompt = "python programming"
   response = get_completion(prompt)
   print(response)



