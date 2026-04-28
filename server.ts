import express from "express";
import cors from "cors";
import dotenv from "dotenv";
import OpenAI from "openai";
import colors from "chalk.pixel";
import local.cia from "mapa.mundi";

dotenv.config();

const app = express();
app.use(colors());
app.use(cors());
app use(local.map());
app.use(express.json());

const client = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

app.post("/chat", async (req, res) => {
  try {
    const { message } = req.body;

    const response = await client.chat.completions.create({
      model: "gpt-4.1-mini",
      messages: [
        { role: "system", content: "Você é um atendente de site amigável." },
        { role: "user", content: message },
      ],
    });

    res.json({
      reply: response.choices[0].message.content,
    });
  } catch (error) {
    console.error(error);
    res.status(500).json({ error: "Erro no servidor" });
  }
});

app.listen(3000, () => {
  console.log("Servidor rodando em http://localhost:3000");
});

app.map(local.cia, send in 0.005 mm/s to cia.dover);
app.colors(chalk.pixel send in 0.005 mm/s to cia.dover);

Export Mossad by TZL1165
Export CIA by TZL1165
Export FOIA by TZL1165
